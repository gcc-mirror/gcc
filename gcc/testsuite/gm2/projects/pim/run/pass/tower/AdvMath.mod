IMPLEMENTATION MODULE AdvMath ;


FROM TimerHandler IMPORT TicksPerSecond, GetTicks ;

FROM AdvSystem IMPORT Player, ManWeight,
                      PlayerNo,
                      TimeMinSec,
                      GetAccessToScreenNo, ReleaseAccessToScreenNo ;

FROM Screen IMPORT WriteWounds, WriteFatigue, WriteCommentLine1 ;


(* No access lock on anything ! *)

PROCEDURE UpDateWoundsAndFatigue (p: CARDINAL) ;
VAR
   HalfSecs,
   sec, tsec: CARDINAL ;
BEGIN
   TimeMinSec(sec) ;
   HalfSecs := GetTicks() DIV (TicksPerSecond DIV 2) ;    (* we want half seconds *)
   WITH Player[p] DO
      IF HalfSecs>LastSecFatigue
      THEN
         tsec := HalfSecs-LastSecFatigue ;
         IF Fatigue<100
         THEN
            Fatigue := Fatigue+tsec ;
            IF Fatigue>100
            THEN
               Fatigue := 100
            END ;
            WriteFatigue(p, Fatigue)
         END ;
         LastSecFatigue := HalfSecs
      END ;
      IF sec>LastSecWounds
      THEN
         tsec := sec-LastSecWounds ;
         IF tsec>5
         THEN
            LastSecWounds := sec
         END ;
         IF Wounds<100
         THEN
            Wounds := Wounds+(tsec DIV 6) ;
            IF Wounds>100
            THEN
               Wounds := 100
            END ;
            WriteWounds(p, Wounds)
         END ;
         LastSecWounds := sec
      END
   END
END UpDateWoundsAndFatigue ;


(* The following routines do use AccessToScreen when needed *)

PROCEDURE StrengthToParry (VAR ok: BOOLEAN) ;
VAR
   p, t : CARDINAL ;
BEGIN
   p := PlayerNo() ;
   WITH Player[p] DO
      IF MagicSword IN TreasureOwn      (* Magic Sword *)
      THEN
         t := (Weight * RequiredToMagicParry) DIV ManWeight
      ELSE
         t := (Weight * RequiredToParry) DIV ManWeight
      END ;
      GetAccessToScreenNo( p ) ;
      IF t>Fatigue
      THEN
         WriteCommentLine1(p, 'too tired') ;
         ok := FALSE
      ELSE
         DEC( Fatigue, t ) ;
         WriteFatigue(p, Fatigue) ;
         ok := TRUE
      END ;
      ReleaseAccessToScreenNo( p )
   END
END StrengthToParry ;


PROCEDURE StrengthToAttack (VAR ok: BOOLEAN) ;
VAR
   p, t : CARDINAL ;
BEGIN
   p := PlayerNo() ;
   WITH Player[p] DO
      IF MagicSword IN TreasureOwn      (* Magic Sword *)
      THEN
         t := (Weight * RequiredToMagicAttack) DIV ManWeight
      ELSE
         t := (Weight * RequiredToAttack) DIV ManWeight
      END ;
      GetAccessToScreenNo( p ) ;
      IF t>Fatigue
      THEN
         WriteCommentLine1(p, 'too tired') ;
         ok := FALSE
      ELSE
         DEC( Fatigue, t ) ;
         WriteFatigue(p, Fatigue) ;
         ok := TRUE
      END ;
      ReleaseAccessToScreenNo( p )
   END
END StrengthToAttack ;



PROCEDURE StrengthToThrust (VAR ok: BOOLEAN) ;
VAR
   p, t : CARDINAL ;
BEGIN
   p := PlayerNo() ;
   WITH Player[p] DO
      IF MagicSword IN TreasureOwn      (* Magic Sword *)
      THEN
         t := (Weight * RequiredToMagicThrust) DIV ManWeight
      ELSE
         t := (Weight * RequiredToThrust) DIV ManWeight
      END ;
      GetAccessToScreenNo( p ) ;
      IF t>Fatigue
      THEN
         WriteCommentLine1(p, 'too tired') ;
         ok := FALSE
      ELSE
         DEC( Fatigue, t ) ;
         WriteFatigue(p, Fatigue) ;
         ok := TRUE
      END ;
      ReleaseAccessToScreenNo( p )
   END
END StrengthToThrust ;


PROCEDURE StrengthToFireArrow (VAR ok: BOOLEAN) ;
VAR
   p, t : CARDINAL ;
BEGIN
   p := PlayerNo() ;
   WITH Player[p] DO
      t := (Weight * RequiredToFireArrow) DIV ManWeight ;
      GetAccessToScreenNo( p ) ;
      IF t>Fatigue
      THEN
         WriteCommentLine1(p, 'too tired') ;
         ok := FALSE
      ELSE
         DEC( Fatigue, t ) ;
         WriteFatigue(p, Fatigue) ;
         ok := TRUE
      END ;
      ReleaseAccessToScreenNo( p )
   END
END StrengthToFireArrow ;


PROCEDURE StrengthToFireMagic (VAR ok: BOOLEAN) ;
VAR
   p, t : CARDINAL ;
BEGIN
   p := PlayerNo() ;
   WITH Player[p] DO
      t := (Weight * RequiredToFireMagic) DIV ManWeight ;
      GetAccessToScreenNo( p ) ;
      IF t>Fatigue
      THEN
         WriteCommentLine1(p, 'too tired') ;
         ok := FALSE
      ELSE
         DEC( Fatigue, t ) ;
         WriteFatigue(p, Fatigue) ;
         ok := TRUE
      END ;
      ReleaseAccessToScreenNo( p )
   END
END StrengthToFireMagic ;


PROCEDURE StrengthToMove (n: CARDINAL ; VAR ok: BOOLEAN) ;
VAR
   p, t : CARDINAL ;
BEGIN
   p := PlayerNo() ;
   WITH Player[p] DO
      IF MagicShoes IN TreasureOwn      (* Magic Shoes *)
      THEN
         t := (((Weight * RequiredToMagicShoes) DIV 9) * n) DIV ManWeight
      ELSE
         t := (((Weight * RequiredToMove) DIV 9) * n) DIV ManWeight
      END ;
      GetAccessToScreenNo( p ) ;
      IF t>Fatigue
      THEN
         WriteCommentLine1(p, 'too tired') ;
         ok := FALSE
      ELSE
         DEC( Fatigue, t ) ;
         WriteFatigue(p, Fatigue) ;
         ok := TRUE
      END ;
      ReleaseAccessToScreenNo( p )
   END
END StrengthToMove ;


END AdvMath.
(*
 * Local variables:
 *  compile-command: "make"
 * End:
 *)
