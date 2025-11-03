-- { dg-do compile }

package Abstract1 is

  type T is abstract tagged null record;

  type S is abstract new T; -- { dg-error "allowed only for record extension" }

end Abstract1;
