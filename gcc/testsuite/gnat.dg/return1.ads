package return1 is
    type Base is abstract tagged null record;
    type Child is new Base with record
       Anon_Access : access Base'Class;
    end record;
    function X_Func (O : access Child) return access Base'Class;
end return1;
