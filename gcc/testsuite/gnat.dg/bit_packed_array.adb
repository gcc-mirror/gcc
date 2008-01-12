-- PR ada/33788
-- Origin: Oliver Kellogg <oliver.kellogg@eads.com>

-- { dg-do compile }

package body Bit_Packed_Array is

  procedure Generate_Callforward is
      Compiler_Crash : String :=
          Laser_Illuminator_Code_Group_T'Image
                (MADR.ISF.Laser_Illuminator_Code (0));
  begin
      null;
  end Generate_Callforward;

end Bit_Packed_Array;
