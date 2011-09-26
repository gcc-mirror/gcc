-- { dg-do compile }
-- { dg-options "-O2 -gnatpn" }

with Opt20_Pkg; use Opt20_Pkg;

package Opt20 is

   procedure Build_Library (For_Project : Integer);

end Opt20;
