--  { dg-do compile }
--  { dg-options "-gnatws" }

package body remote_type is
   procedure Append
     (Container : in out List;
      New_Item  : in     Element_Type)
   is
   begin
      null;
   end Append;
   procedure Read
     (S : access Root_Stream_Type'Class;
      L : out List)
   is
   begin
      null;
   end Read;
   procedure Write
     (S : access Root_Stream_Type'Class;
      L : in List)
   is
   begin
      null;
   end Write;
end remote_type;
