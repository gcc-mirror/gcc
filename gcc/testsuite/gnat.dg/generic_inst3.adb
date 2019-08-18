--  { dg-do compile }

with Generic_Inst3_Kafka_Lib.Topic;
with Generic_Inst3_Traits.Encodables;
with Generic_Inst3_Markets;

procedure Generic_Inst3 is
   generic
      with package Values is new Generic_Inst3_Traits.Encodables (<>);
      with package Topic is new Generic_Inst3_Kafka_Lib.Topic
          (Values => Values, others => <>);
   package Dummy is
   end Dummy;

   package Inst is new Dummy
      (Values => Generic_Inst3_Markets.Data_Encodables,
       Topic  => Generic_Inst3_Markets.Data_Topic);
begin
   null;
end Generic_Inst3;
