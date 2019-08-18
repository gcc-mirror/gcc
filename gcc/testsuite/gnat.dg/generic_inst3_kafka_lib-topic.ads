with Generic_Inst3_Traits.Encodables;
generic
   Topic_Name : String;
   with package Keys is new Generic_Inst3_Traits.Encodables (<>);
   with package Values is new Generic_Inst3_Traits.Encodables (<>);
package Generic_Inst3_Kafka_Lib.Topic is
end Generic_Inst3_Kafka_Lib.Topic;
