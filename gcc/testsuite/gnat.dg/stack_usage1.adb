-- { dg-do compile }
-- { dg-options "-Wstack-usage=128" { target i?86-*-* x86_64-*-* } }

with Stack_Usage1_Pkg; use Stack_Usage1_Pkg;

procedure Stack_Usage1 is

   A : Integer := Ident_Int (123);

begin
   case A is
       when 0 =>
          My_Proc (R'(Ident_Int(0), Ident_Int(1), Ident_Int(2), Ident_Int(3), Ident_Int(4), Ident_Int(5), Ident_Int(6), Ident_Int(7), Ident_Int(8), Ident_Int(9)));
       when 1 =>
          My_Proc (R'(Ident_Int(0), Ident_Int(1), Ident_Int(2), Ident_Int(3), Ident_Int(4), Ident_Int(5), Ident_Int(6), Ident_Int(7), Ident_Int(8), Ident_Int(9)));
       when 2 =>
          My_Proc (R'(Ident_Int(0), Ident_Int(1), Ident_Int(2), Ident_Int(3), Ident_Int(4), Ident_Int(5), Ident_Int(6), Ident_Int(7), Ident_Int(8), Ident_Int(9)));
       when 3 =>
          My_Proc (R'(Ident_Int(0), Ident_Int(1), Ident_Int(2), Ident_Int(3), Ident_Int(4), Ident_Int(5), Ident_Int(6), Ident_Int(7), Ident_Int(8), Ident_Int(9)));
       when 4 =>
          My_Proc (R'(Ident_Int(0), Ident_Int(1), Ident_Int(2), Ident_Int(3), Ident_Int(4), Ident_Int(5), Ident_Int(6), Ident_Int(7), Ident_Int(8), Ident_Int(9)));
       when 5 =>
          My_Proc (R'(Ident_Int(0), Ident_Int(1), Ident_Int(2), Ident_Int(3), Ident_Int(4), Ident_Int(5), Ident_Int(6), Ident_Int(7), Ident_Int(8), Ident_Int(9)));
       when 6 =>
          My_Proc (R'(Ident_Int(0), Ident_Int(1), Ident_Int(2), Ident_Int(3), Ident_Int(4), Ident_Int(5), Ident_Int(6), Ident_Int(7), Ident_Int(8), Ident_Int(9)));
       when 7 =>
          My_Proc (R'(Ident_Int(0), Ident_Int(1), Ident_Int(2), Ident_Int(3), Ident_Int(4), Ident_Int(5), Ident_Int(6), Ident_Int(7), Ident_Int(8), Ident_Int(9)));
       when 8 =>
          My_Proc (R'(Ident_Int(0), Ident_Int(1), Ident_Int(2), Ident_Int(3), Ident_Int(4), Ident_Int(5), Ident_Int(6), Ident_Int(7), Ident_Int(8), Ident_Int(9)));
       when 9 =>
          My_Proc (R'(Ident_Int(0), Ident_Int(1), Ident_Int(2), Ident_Int(3), Ident_Int(4), Ident_Int(5), Ident_Int(6), Ident_Int(7), Ident_Int(8), Ident_Int(9)));
       when others =>
          null;
   end case;

end Stack_Usage1;
