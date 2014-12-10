-- { dg-do compile }

procedure Entry_Queues2 is

    F1 : Integer := 17;

    generic
        type T is limited private;
    procedure Check;

    procedure Check is
    begin
        declare
            type Poe is new T;
        begin
            declare
                type Arr is array (1 .. 2) of Poe;
                X : Arr;
                pragma Unreferenced (X);
            begin
                null;
            end;
        end;
    end;

begin

    declare
        protected type Poe (D3 : Integer := F1) is
            entry E (D3 .. F1);    -- F1 evaluated
        end Poe;
        protected body Poe is
            entry E (for I in D3 .. F1) when True is
            begin
                null;
            end E;
        end Poe;

        procedure Chk is new Check (Poe);

    begin
        Chk;
    end;

end;
