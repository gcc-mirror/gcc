-- { dg-do compile }
-- { dg-options "-gnatc" }

limited with Limited_With4;

package Limited_With4_Pkg is

   type Object is tagged null record;
   type Object_Ref is access all Object;
   type Class_Ref is access all Object'Class;

   function Func return Limited_With4.Class_Ref;
   procedure Proc (Arg : Limited_With4.Class_Ref);

end Limited_With4_Pkg;
