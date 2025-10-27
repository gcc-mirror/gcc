-- PR ada/120665
-- { dg-do compile }
-- { dg-options "-gnat2022" }

package Aggr8 is

  type T is null record
    with Aggregate => (Empty => Empty, Add_Named => Add_Named);

  function Empty return T is ([]);  -- { dg-warning "empty|infinite" }

  procedure Add_Named (this : in out T; k : Integer; v : Integer) is null;

end Aggr8;
