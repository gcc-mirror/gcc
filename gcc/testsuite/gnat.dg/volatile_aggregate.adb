-- { dg-do compile }

with System;

procedure Volatile_Aggregate is 

  function GetArrayUpperBound return Integer is 
  begin
    return 2;
  end GetArrayUpperBound; 

  some_value : Integer := GetArrayUpperBound;

  type Gp_Element_Type is record
    Element : Integer;
  end record;

  type some_type is array (1 .. some_value) of Gp_Element_Type;

  type Aligned_Some_Type is record
    Value : aliased some_type;
  end record;          

  for Aligned_Some_Type'Alignment use 8;          

  an_aligned_type : aligned_Some_Type;   
  my_address : system.address; 

  pragma Volatile (an_aligned_type);

begin
  my_address := an_aligned_type.value(1)'address; 
end;      
