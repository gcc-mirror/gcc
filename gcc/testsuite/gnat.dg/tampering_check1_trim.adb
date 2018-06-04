procedure Tampering_Check1_Trim
  (V : in out Tampering_Check1_IVectors.Vector) is
   use Tampering_Check1_IVectors;

begin
   while not Is_Empty (V) and then V (V.First) < 0 loop
      V.Delete_First;
   end loop;
end Tampering_Check1_Trim;
