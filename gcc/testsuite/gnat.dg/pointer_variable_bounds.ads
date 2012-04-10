with pointer_variable_bounds_q; use pointer_variable_bounds_q;

package pointer_variable_bounds is

  type HALF_INTEGER is range -32768 .. 32767;
  subtype HALF_NATURAL is HALF_INTEGER range 0 .. 32767;

  MAX_COMPS : constant HALF_NATURAL := HALF_NATURAL(A_MAX_COMPS);
  subtype COMP_POINTER_TYPE is HALF_NATURAL range 0 .. MAX_COMPS;
  subtype BUNDLE_POINTER_TYPE is HALF_NATURAL range 0 .. 1;
  subtype C_POINTER_TYPE is HALF_NATURAL range 0 .. 1;

  procedure BUNDLE_DAT(BP : in BUNDLE_POINTER_TYPE);
  procedure SEQUENCE_DAT(BP : in BUNDLE_POINTER_TYPE);

end pointer_variable_bounds;
