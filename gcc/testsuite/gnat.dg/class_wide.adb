-- { dg-do compile }

procedure class_wide is
    package P is
       type T is tagged null record;
       procedure P1 (x : T'Class);
       procedure P2 (x : access T'Class);
    end P;
    package body P is
        procedure P1 (x : T'Class) is 
       begin 
          null;
       end;
       procedure P2 (x : access T'Class) is
       begin
          null;
       end;
    end P;
    use P;
    a : T;
    type Ptr is access T;
    b : Ptr := new T;
begin
    A.P1;
    B.P2;
end;
