-- { dg-do compile }
-- { dg-options "-gnato" }

procedure Dynamic_Bound is

  procedure Define (Count : Integer) is

    type Count_T is new Integer range 0 .. Count * 1000;

    type Obj_T is record
      Count : Count_T;
    end record;

    type T is access Obj_T ;

    procedure Create (S : in out T) is
    begin
      S := new Obj_T'(Count => 0);
    end;

    procedure Add (To : in out T) is
    begin
      To.Count := To.Count + 1;
    end;

    My_T : T;

  begin
    Create (My_T);
  end;

begin
  Define (1);
end;
