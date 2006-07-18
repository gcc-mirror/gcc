-- { dg-do compile }
-- { dg-options "-gnatws" }
--              (bits of "Header" unused)

procedure Nested_Agg_Bitfield_Constructor is

    type Uint64 is mod 2 ** 64;
    type Uint16 is mod 2 ** 16;

    type Time_Stamp is record
       Sec  : Uint64;
       Year : Uint16;
    end record;

    type Msg_Header is record
       Stamp : Time_Stamp;
    end record;
    for Msg_Header use record
       Stamp at 0 range 0 .. 64+16-1;
    end record;
    for Msg_Header'Size use 80;

    type Msg is record
       Header : Msg_Header;
    end record;

    for Msg use record
       Header at 0 range 0 .. 191;
    end record;

    M : Msg := (Header => (Stamp => (2, 4)));
begin
   null;
end;
