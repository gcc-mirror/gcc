--  { dg-do compile }

package Self_Class is
   type P6 is private;
private
   type P6 is tagged record
      Self : access P6'Class;
   end record;
end Self_Class;
