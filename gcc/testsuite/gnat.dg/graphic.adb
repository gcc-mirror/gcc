-- { dg-do compile }

with Ada.Tags.Generic_Dispatching_Constructor;
package body Graphic is
--
  function Dispatching_Input is new Tags.Generic_Dispatching_Constructor
    (T           => Object,
     Parameters  => Streams.Root_Stream_Type'Class,
    Constructor => Object'Input);
--
  function XML_Input
    (S : access Streams.Root_Stream_Type'Class) return Object'Class
  is
    Result : constant Object'Class :=
        Dispatching_Input (Tags.Internal_Tag (" "), S);
    begin
      return Result;
    end XML_Input;
end Graphic;

