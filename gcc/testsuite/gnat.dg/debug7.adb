-- { dg-do compile }
-- { dg-skip-if "No dwarf-2 support" { hppa*-*-hpux* } }
-- { dg-options "-cargs -gdwarf-2 -gstrict-dwarf -dA -margs" }
-- { dg-final { scan-assembler "DW_TAG_imported_decl" } }

package body Debug7 is
   function Next (I : Integer) return Integer is
   begin
      return I + 1;
   end Next;
end Debug7;
