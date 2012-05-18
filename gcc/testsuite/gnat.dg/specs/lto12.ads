-- { dg-do compile }
-- { dg-options "-flto" { target lto } }

with Lto12_Pkg; use Lto12_Pkg;

package Lto12 is

  C : constant R := F;

end Lto12;
