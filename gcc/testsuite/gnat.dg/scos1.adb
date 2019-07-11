--  { dg-do compile }
--  { dg-options "-gnata -gnateS" }

procedure SCOs1 with SPARK_Mode => On is

   LEN_IN_BITS : constant := 20;

   M_SIZE_BYTES : constant := 2 ** LEN_IN_BITS;
   ET_BYTES : constant := (M_SIZE_BYTES - 4);

   type T_BYTES  is new Integer range 0 .. ET_BYTES  with Size => 32;
   subtype TYPE5_SCALAR is T_BYTES
     with Dynamic_Predicate => TYPE5_SCALAR mod 4 = 0;

   type E_16_BYTES is new Integer;
   subtype RD_BYTES is E_16_BYTES
     with Dynamic_Predicate => RD_BYTES mod 4 = 0;

   function "-" (left : TYPE5_SCALAR; right : RD_BYTES) return TYPE5_SCALAR
   is ( left - TYPE5_SCALAR(right) )
     with Pre => TYPE5_SCALAR(right) <= left and then
     left - TYPE5_SCALAR(right) <= T_BYTES'Last, Inline_Always;

begin
   null;
end SCOs1;
