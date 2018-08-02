--  { dg-do compile }

procedure Generic_Call_Iface is

   generic
     type Subscriber_Type is interface;
     with procedure On_Changed (Subscriber : in out Subscriber_Type)
       is abstract;
   package My_Generic is
      type Subscriber_Ptr is access all Subscriber_Type'Class;
      procedure Update;
      Subscriber : Subscriber_Ptr := null;
   end;

   package body My_Generic is
      procedure Update is
      begin
         if Subscriber /= null then
            Subscriber.On_Changed;
         end if;
      end;
   end;

   package User is
      type Integer_Subscriber is interface;
      procedure On_Changed_Int (I : in out Integer_Subscriber) is abstract;

      package P is new My_Generic
        (Subscriber_Type => Integer_Subscriber,
         On_Changed      => On_Changed_Int);
   end;
begin
   null;
end;
