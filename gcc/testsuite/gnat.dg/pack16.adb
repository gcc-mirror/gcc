-- { dg-do compile }
-- { dg-options "-gnatws" }

with Pack16_Pkg; use Pack16_Pkg;

procedure Pack16 is

   type Sample_Table_T is array (1 .. N) of Integer;

   type Clock_T is record
      N_Ticks  : Integer := 0;
   end record;

   type Sampling_Descriptor_T is record
      Values : Sample_Table_T;
      Valid  : Boolean;
      Tstamp : Clock_T;
   end record;

   pragma Pack (Sampling_Descriptor_T);

   Sampling_Data : Sampling_Descriptor_T;

begin
   null;
end;
