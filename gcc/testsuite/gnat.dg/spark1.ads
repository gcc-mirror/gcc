package Spark1 is

   Mailbox : Integer with Atomic, Async_Writers, Async_Readers;

   task Worker
     with Global => (Input => Mailbox);

end;
