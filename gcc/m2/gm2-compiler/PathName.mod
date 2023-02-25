IMPLEMENTATION MODULE PathName ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM DynamicStrings IMPORT InitString, ConCat, ConCatChar, char, Dup,
                           KillString, Length, EqualArray, Equal, Mark ;
FROM SFIO IMPORT Exists ;
FROM FIO IMPORT StdErr ;
FROM M2Printf IMPORT fprintf0, fprintf1, fprintf2 ;
FROM FormatStrings IMPORT Sprintf1 ;

FROM DynamicPath IMPORT InitPathList, FindFileName ;

IMPORT DynamicPath ;


CONST
   Debugging = FALSE ;

TYPE
   NamedPath = POINTER TO RECORD
                             pathList: PathList ;
                             name    : String ;
                             tail,
                             next    : NamedPath ;
                          END ;


VAR
   FreeList,
   NamedPathHead: NamedPath ;



(*
   AddSystem -
*)

PROCEDURE AddSystem (named, directory: String) ;
BEGIN
   IF NamedPathHead = NIL
   THEN
      (* Empty dictionary add single entry.  *)
      SetNamedPath (InitNamedPath (named, InitPathList (directory)))
   ELSIF Equal (NamedPathHead^.tail^.name, named)
   THEN
      NamedPathHead^.tail^.pathList := DynamicPath.Cons (NamedPathHead^.tail^.pathList,
                                                         directory)
   ELSE
      SetNamedPath (ConsList (NamedPathHead,
                              InitNamedPath (named, InitPathList (directory))))
   END
END AddSystem ;


(*
   AddUser -
*)

PROCEDURE AddUser (named, directory: String) ;
BEGIN
   IF NamedPathHead = NIL
   THEN
      (* Empty dictionary add single entry.  *)
      SetNamedPath (InitNamedPath (named, InitPathList (directory)))
   ELSIF EqualArray (NamedPathHead^.name, '')
   THEN
      (* Found user node.  *)
      NamedPathHead^.pathList := DynamicPath.Cons (NamedPathHead^.pathList,
                                                   directory)
   ELSE
      (* No user node yet, so we will create one.  *)
      NamedPathHead := ConsList (InitNamedPath (named, InitPathList (directory)),
                                 NamedPathHead) ;
      SetNamedPath (NamedPathHead)
   END
END AddUser ;


(*
   AddInclude - adds include path to the named path.  If named path
                is the same as the previous call then the include path
                is appended to the named path PathList otherwise a new
                named path is created and placed at the end of the
                named path list.
*)

PROCEDURE AddInclude (named, directory: String) ;
BEGIN
   IF Debugging
   THEN
      fprintf2 (StdErr, "named = %s, directory =%s\n",
                named, directory)
   END ;
   IF (named = NIL) OR EqualArray (named, '')
   THEN
      AddUser (named, directory) ;
      IF Debugging
      THEN
         DumpPathName ('User pathname')
      END
   ELSE
      AddSystem (named, directory) ;
      IF Debugging
      THEN
         DumpPathName ('System pathname')
      END
   END
END AddInclude ;


(*
   SetNamedPath - assigns the named path to the default path.
*)

PROCEDURE SetNamedPath (named: NamedPath) ;
BEGIN
   NamedPathHead := named
END SetNamedPath ;


(*
   GetNamedPath - returns the default named path.
*)

PROCEDURE GetNamedPath () : NamedPath ;
BEGIN
   RETURN NamedPathHead
END GetNamedPath ;


(*
   KillNamedPath - places list np onto the freelist.
                   Postcondition: np will be NIL.
*)

PROCEDURE KillNamedPath (VAR np: NamedPath) ;
BEGIN
   IF np # NIL
   THEN
      np^.tail^.next := FreeList ;
      FreeList := np ;
      np := NIL
   END
END KillNamedPath ;


(*
   ConsList - concatenates named path left and right together.
*)

PROCEDURE ConsList (left, right: NamedPath) : NamedPath ;
BEGIN
   IF right # NIL
   THEN
      left^.tail^.next := right ;
      left^.tail := right^.tail
   END ;
   RETURN left
END ConsList ;


(*
   Cons - appends pl to the end of a named path.
          If np is NIL a new list is created and returned
          containing named and pl.
*)

PROCEDURE Cons (np: NamedPath; named: String; pl: PathList) : NamedPath ;
BEGIN
   IF np = NIL
   THEN
      np := InitNamedPath (named, pl)
   ELSE
      np := ConsList (np, InitNamedPath (named, pl))
   END ;
   RETURN np
END Cons ;


(*
   Stash - returns np before setting np to NIL.
*)

PROCEDURE Stash (VAR np: NamedPath) : NamedPath ;
VAR
   old: NamedPath ;
BEGIN
   old := np ;
   np := NIL ;
   RETURN old
END Stash ;


(*
   InitNamedPath - creates a new path name with an associated pathlist.
*)

PROCEDURE InitNamedPath (name: String; pl: PathList) : NamedPath ;
VAR
   np: NamedPath ;
BEGIN
   NEW (np) ;
   IF np = NIL
   THEN
      HALT
   ELSE
      np^.pathList := pl ;
      np^.name := Dup (name) ;
      np^.next := NIL ;
      np^.tail := np
   END ;
   RETURN np
END InitNamedPath ;


(*
   FindNamedPathFile - Post-condition: returns NIL if a file cannot be found otherwise
                       it returns the path including the filename.
                       It also returns a new string the name of the path.
                       Pre-condition: if name = NIL then it searches
                                          user path first, followed by any
                                          named path.
                                      elsif name = ''
                                      then
                                         search user path
                                      else
                                         search named path
                                      fi
*)

PROCEDURE FindNamedPathFile (filename: String; VAR name: String) : String ;
VAR
   foundFile: String ;
   np       : NamedPath ;
BEGIN
   np := NamedPathHead ;
   WHILE np # NIL DO
      IF (name = NIL) OR Equal (np^.name, name)
      THEN
         foundFile := FindFileName (filename, np^.pathList) ;
         IF foundFile # NIL
         THEN
            name := Dup (np^.name) ;
            RETURN foundFile
         END
      END ;
      np := np^.next
   END ;
   name := NIL ;
   RETURN NIL
END FindNamedPathFile ;


(*
   DumpPathName - display the dictionary of names and all path entries.
*)

PROCEDURE DumpPathName (name: ARRAY OF CHAR) ;
VAR
   np    : NamedPath ;
   leader: String ;
BEGIN
   fprintf0 (StdErr, name) ;
   fprintf0 (StdErr, " = {\n") ;
   np := NamedPathHead ;
   WHILE np # NIL DO
      leader := Sprintf1 (Mark (InitString ("  %s")), np^.name) ;
      DynamicPath.DumpPath (leader, np^.pathList) ;
      leader := KillString (leader) ;
      np := np^.next
   END ;
   fprintf0 (StdErr, "}\n")
END DumpPathName ;


BEGIN
   NamedPathHead := NIL ;
   FreeList := NIL
END PathName.
