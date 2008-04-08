with Ada.Streams; use Ada.Streams;
package RT1 is
   pragma Remote_Types;

   type Ptr is private;
   procedure Read (X : access Root_Stream_Type'Class; V : out Ptr) is null;
   procedure Write (X : access Root_Stream_Type'Class; V : Ptr) is null;
   for Ptr'Read use Read;
   for Ptr'Write use Write;
   
   procedure P (S : access Root_Stream_Type'Class);
private
   type Ptr is not null access all Integer;
end RT1;
