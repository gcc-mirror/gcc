generic
   type Component is private;
package G_Tables is
   type Table (<>) is limited private;

   function  Create (L : Natural) return Table;
private
   type Table is array (Positive range <>) of Component;
end G_Tables;
