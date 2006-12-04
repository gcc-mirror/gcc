-- { dg-do compile }
-- { dg-options "-gnatws" }

package body pointer_variable_bounds is

  function COMPONENT_DAT(BP : in BUNDLE_POINTER_TYPE; CP : in COMP_POINTER_TYPE) return HALF_INTEGER is
    type CP_TYPE is access COMP_POINTER_TYPE;
    type CD_TYPE is access HALF_INTEGER;
    CD : CD_TYPE;
  begin
    return CD.all;
  end;

  procedure BUNDLE_DAT(BP : in BUNDLE_POINTER_TYPE) is
    N0 : C_POINTER_TYPE := COMPONENT_DAT(BP, 4);
  begin
    null;
  end;

  procedure SEQUENCE_DAT(BP : in BUNDLE_POINTER_TYPE) is
    N0 : C_POINTER_TYPE := COMPONENT_DAT(BP, 4);
  begin
    null;
  end;

end pointer_variable_bounds;
