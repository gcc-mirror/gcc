--  { dg-do compile }

with Class_Wide6_Pkg;

package Class_Wide6 is

   type Script_Kind_Enum is (Transformer, Validator);

   type Script_Info (Script_Kind : Script_Kind_Enum) is tagged null record;

   package Base_Script_Info_Node is new Class_Wide6_Pkg (Script_Info'Class);

   type Script_Info_Parser is new Base_Script_Info_Node.Base_Node_Parser with
      null record;

   overriding function Parse (Parser: Script_Info_Parser)
                              return Script_Info'Class;

end Class_Wide6;
