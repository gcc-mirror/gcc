with Ada.Streams;
with Ada.Tags;
package Graphic is
  use Ada;
--
  type Object is abstract tagged null record;
  function XML_Input (S : access Streams.Root_Stream_Type'Class)
     return Object'Class;
end Graphic;
