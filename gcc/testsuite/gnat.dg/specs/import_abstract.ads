-- { dg-do compile }
package Import_Abstract is
   type T1 is abstract tagged null record;
   procedure p1(X : T1) is abstract;
   pragma Import (Ada, p1);    -- { dg-error "cannot import abstract subprogram" }
end Import_Abstract;
