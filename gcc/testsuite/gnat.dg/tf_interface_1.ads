with Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
package TF_INTERFACE_1 is

   type CF_INTERFACE_1 is interface;

   procedure P_PROCEDURE_1 (This : in out CF_INTERFACE_1)
   is abstract;

   procedure Read (Stream : not null access ada.Streams.Root_stream_Type'Class;
                   Item : out CF_INTERFACE_1) is null;
   for CF_INTERFACE_1'Read use Read;

   procedure Write (Stream : not null access ada.Streams.Root_stream_Type'Class;
                   Item : CF_INTERFACE_1) is null;
   for CF_INTERFACE_1'Write use Write;

   procedure Get_It (Handle : Stream_Access; It : out CF_Interface_1'class);
end TF_INTERFACE_1;
