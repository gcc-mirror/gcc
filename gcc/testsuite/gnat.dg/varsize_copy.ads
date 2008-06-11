package Varsize_Copy is

   type Key_Type is
      (Nul, Cntrl, Stx, Etx, Eot, Enq, Ack, Spad, Clr, Dc_1, Dc_2, Dc_3, Dc_4);

   for Key_Type use
      (Nul   => 0,
       Cntrl => 1,
       Stx   => 2,
       Etx   => 3,
       Eot   => 4,
       Enq   => 5,
       Ack   => 6,
       Spad  => 7,
       Clr   => 8,
       Dc_1  => 17,
       Dc_2  => 18,
       Dc_3  => 19,
       Dc_4  => 20);

   type Page_Type(D : Boolean := False) is record
      case D is
         when True => I : Integer;
         when False => null;
      end case;
   end record;

   function F (Key : Key_Type) return Page_Type;

end Varsize_Copy;
