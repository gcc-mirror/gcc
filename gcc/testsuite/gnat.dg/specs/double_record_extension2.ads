-- { dg-do compile } 

package double_record_extension2 is

  type Base_Message_Type (Num_Bytes : Positive) is tagged record
     Data_Block : String (1..Num_Bytes);
  end record;

  type Extended_Message_Type (Num_Bytes1 : Positive; Num_Bytes2 : Positive) is new Base_Message_Type (Num_Bytes1) with record
     A: String (1..Num_Bytes2);
  end record;

  type Final_Message_Type is new Extended_Message_Type with record
     B : Integer;
  end record;

end double_record_extension2;
