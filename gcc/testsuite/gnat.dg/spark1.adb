--  { dg-do compile }

package body Spark1 is

   task body Worker is

      procedure Update with
        Global => (In_Out => Mailbox) --  { dg-error "global item \"Mailbox\" cannot have mode In_Out or Output|item already appears as input of task \"Worker\"" }
      is
         Tmp : Integer := Mailbox;
      begin
         Mailbox := Tmp + 1;
      end Update;

      X : Integer := Mailbox;
   begin
      loop
         Update;
      end loop;
   end;

end;
