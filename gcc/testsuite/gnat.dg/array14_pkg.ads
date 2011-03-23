with System.Storage_Elements;

package Array14_Pkg is

  package SSE renames System.Storage_Elements;

  function Parity_Byte_Count return SSE.Storage_Count;

  Length2 : constant SSE.Storage_Count := Parity_Byte_Count;

  subtype Encoded_Index_Type2 is SSE.Storage_Count range 1 .. Length2;
  subtype Encoded_Type2 is SSE.Storage_Array (Encoded_Index_Type2'Range);

  procedure Encode2 (Input : in Integer; Output : out Encoded_Type2);

end Array14_Pkg;
