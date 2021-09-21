--  { dg-do compile }
--  { dg-additional-sources asan1_pkg.ads }
--  { dg-options "-fsanitize=address" }
--  { dg-skip-if "no address sanitizer" { no_fsanitize_address } }

with Asan1_Pkg;

procedure Asan1 is
   use Asan1_Pkg;

   X, Y : E;
begin
   X := C (N);
   Y := V;
end Asan1;
