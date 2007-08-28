with Ada.Streams;
generic
   type Element_Type is private;
package remote_type is
   pragma Remote_Types;
   type List is private;
   procedure Append
     (Container : in out List;
      New_Item  : in     Element_Type);
private
   use Ada.Streams;
   type List_Record is record
      A : Boolean;
   end record;
   type List is access List_Record;
   procedure Read
     (S : access Root_Stream_Type'Class;
      L : out List);
   for List'Read use Read;
   procedure Write
     (S : access Root_Stream_Type'Class;
      L : in List);
   for List'Write use Write;
end remote_type;
