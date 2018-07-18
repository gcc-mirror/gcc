--  { dg-do compile }

procedure Tagged_Prefix_Call is

   package Defs is
      type Database_Connection_Record is abstract tagged null record;
      type Database_Connection is access all Database_Connection_Record'Class;

      procedure Start_Transaction
        (Self : not null access Database_Connection_Record'Class)
      is null;

      type DB_Connection (Elem : access Database_Connection)
      is null record
        with Implicit_Dereference => Elem;
   end Defs;

   use Defs;

   DB  : DB_Connection(null);

begin
   DB.Start_Transaction;
end Tagged_Prefix_Call;
