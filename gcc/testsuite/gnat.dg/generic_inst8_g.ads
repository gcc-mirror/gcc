generic
   N : Natural;
package Generic_Inst8_G is

   generic
      type T is private;
   package First is
      function Get (Data : T) return T with Inline;
   end First;

   generic
      type T is private;
   package Second is
      package My_First is new First (T);
   end Second;

end Generic_Inst8_G;
