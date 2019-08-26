generic
  type Field_Type is limited private;
package Prot9_Gen is

  type Field_Pointer is access all Field_Type;

  Pointer : Field_Pointer := new Field_Type;

end Prot9_Gen;
