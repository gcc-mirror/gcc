struct S { @disable this(); }
// Does not compile: "default construction is disabled for type `S`"
extern __gshared S gVariable1;
