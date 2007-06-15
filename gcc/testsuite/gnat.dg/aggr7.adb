--  { dg-do compile }

procedure aggr7 is
   
   package P is
      type T is limited private;
      type TT is limited private;
      type TTT is tagged limited private;
   private
      type T is limited
          record
             Self : access T := T'Unchecked_Access;
          end record;
      type TT is tagged limited
          record
             Self : access TT := TT'Unchecked_Access;
          end record;
      type TTT is tagged limited
          record
             Self : access TTT := TTT'Unchecked_Access;
          end record;
   end P;
   
   package body P is
      X : T := (Self => <>);
      XX : TT := (Self => <>);
      XXX : TTT := (Self => <>);
      Y : T := (others => <>);
      YY : TT := (others => <>);
      YYY : TTT := (others => <>);
   end P;
begin
   null;
end aggr7;
