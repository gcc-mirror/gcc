--  { dg-do compile }

procedure interface4 is
     generic
       type I1 is interface;
       type I2 is limited interface;
       type I3 is interface and I1;
       type I4 is limited interface and I2;
     package Pack_I is
     end Pack_I;
begin
     null;
end interface4;
