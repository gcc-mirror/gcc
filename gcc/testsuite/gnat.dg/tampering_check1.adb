--  { dg-do run }

with Tampering_Check1_IVectors; use Tampering_Check1_IVectors;
with Tampering_Check1_Trim;

procedure Tampering_Check1 is
   V : Vector;

begin
   V.Append (-1);
   V.Append (-2);
   V.Append (-3);

   Tampering_Check1_Trim (V);
end Tampering_Check1;
