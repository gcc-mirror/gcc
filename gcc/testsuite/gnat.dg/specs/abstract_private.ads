generic
package Abstract_Private is

   type T1 is abstract tagged private;
   procedure P1 (X : T1) is abstract;

   type T2 is abstract tagged private;

private

   type T1 is abstract tagged null record;
   procedure P2 (X : T1) is abstract; -- { dg-error "must be visible" }

   type T2 is abstract new T1 with null record;
   procedure P1 (X : T2) is abstract;

end Abstract_Private;
