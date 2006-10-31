package inline_scope_p is
    procedure Assert (Expr : Boolean; Str : String);
    pragma Inline (Assert);
end;    
