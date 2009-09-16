package Discr20 is

  Size : Integer;

  type Name is new String (1..Size);

  type Rec is record
     It : Name;
  end record;

  type Danger is (This, That);
  type def (X : Danger := This) is record
    case X is
       when This => It : Rec;
       when That => null;
       end case;
   end record;

   type Switch is (On, Off);
   type Wrapper (Disc : Switch := On) is private;
   function Get (X : Wrapper) return Def;

private
   type Wrapper (Disc : Switch := On) is record
      Case Disc is
         when On  => It : Def;
         when Off => null;
      end case;
   end record;

end Discr20;
