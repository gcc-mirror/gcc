-- { dg-do compile }
-- { dg-final { scan-assembler-not "elabs" } }

package OCONST6 is

   type Sequence is array (1 .. 1) of Natural;

   type Message is record
      Data : Sequence;
   end record;

   for Message'Alignment use 1;
   pragma PACK (Message);

   ACK : Message := (Data => (others => 1));

end;

