-- { dg-do compile }
-- { dg-options "-gnatX0" }

package Finalizable1 is

  type Root is abstract tagged null record
    with Finalizable => (Finalize => Finalize);

  procedure Finalize (This : in out Root) is abstract;

end Finalizable1;
