private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Unbounded;

package Generic_Inst15_Pkg is
   type Word_Order is
     (wo_Alpha,
      wo_Position,
      wo_Frequency_Alpha,
      wo_Frequency_Position);

   subtype Book_Code_Type is String (1 .. 24);

   type Word_Type is private;
   type Word_Status is (ws_Single, ws_Multi, ws_Not_All, ws_Unknown);
   type Translation_Index is new Natural range 1 .. 10;

   function Get_Word (Self : in Word_Type) return String;

   type Book_Type is private;

private

   package Translation_List is new Ada.Containers.Indefinite_Vectors (
      Index_Type   => Translation_Index,
      Element_Type => String,
      "="          => "=");

   type Word_Type is record
      Is_All : Boolean := False;
      Translations : Translation_List.Vector;
   end record;

   type Book_Type is record
      Line  : Positive := 1;
      Index : Positive := 1;
   end record;
end Generic_Inst15_Pkg;
