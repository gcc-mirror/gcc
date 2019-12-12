with Generic_Inst3_Kafka_Lib.Topic;
with Generic_Inst3_Traits.Encodables;
package Generic_Inst3_Markets is
   type Data_Type is null record;
   function Image (D : Data_Type) return String is ("");
   package Data_Encodables is new Generic_Inst3_Traits.Encodables (Data_Type, Image);
   package Data_Topic is new Generic_Inst3_Kafka_Lib.Topic
      (Keys => Data_Encodables, Values => Data_Encodables,
       Topic_Name => "bla");
end Generic_Inst3_Markets;
