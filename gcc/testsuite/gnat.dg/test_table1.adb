--  { dg-do run }

with GNAT.Table;
with Ada.Text_IO; use Ada.Text_IO;
 
procedure test_table1 is
   type Rec is record
     A, B, C, D, E : Integer := 0;
     F, G, H, I, J : Integer := 1;
     K, L, M, N, O : Integer := 2;
   end record;
   
   R : Rec;
        
   package Tab is new GNAT.Table (Rec, Positive, 1, 4, 30);
        
   Last : Natural;
        
begin   
   R.O := 3;
        
   Tab.Append (R);

   for J in 1 .. 1_000_000 loop
      Last := Tab.Last;
      begin
         Tab.Append (Tab.Table (Last));
      exception
         when others =>
             Put_Line ("exception raise for J =" & J'Img);
             raise;
      end;

      if Tab.Table (Tab.Last) /= R then
         Put_Line ("Last is not what is expected");
         Put_Line (J'Img);
         return;
      end if;
   end loop;
end;
