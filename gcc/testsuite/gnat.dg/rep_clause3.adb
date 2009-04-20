-- { dg-do compile }
-- { dg-options "-gnatws" }

procedure Rep_Clause3 is

   subtype U_16 is integer range 0..2**16-1;

   type TYPE1 is range 0 .. 135;
   for TYPE1'size use 14;

   type TYPE2 is range 0 .. 262_143;
   for TYPE2'size use 18;

   subtype TYPE3 is integer range 1 .. 21*6;

   type ARR is array (TYPE3 range <>) of boolean;
   pragma Pack(ARR);

   subtype SUB_ARR is ARR(1 .. 5*6);

   OBJ  : SUB_ARR;

   type R is
    record
      N   : TYPE1;
      L   : TYPE2;
      I   : SUB_ARR;
      CRC : U_16;
     end record;
    for R use
     record at mod 4;
      N   at  0 range  0 .. 13;
      L   at  0 range 14 .. 31;
      I   at  4 range  2 .. 37;
      CRC at  8 range 16 .. 31;
     end record;
   for R'size use 12*8;

   type SUB_R is array (1..4) of R;

   T : SUB_R;

begin
  if OBJ = T(1).I then
    raise Program_Error;
  end if;
end;
