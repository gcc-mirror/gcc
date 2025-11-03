package body Vect19_Pkg is

  function Sum (X : Arr; Y : Arr) return Arr is
    Result : Arr;
  begin
    for I in X'Range loop
      Result(I) := X(I) + Y(I);
    end loop;
    return Result;
  end;

end Vect19_Pkg;
