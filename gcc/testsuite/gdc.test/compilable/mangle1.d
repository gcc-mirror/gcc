//spaces
__gshared pragma(mangle, "test 9") ubyte test9_1;
__gshared extern pragma(mangle, "test 9") ubyte test9_1_e;

//\n chars
__gshared pragma(mangle, "test\\\n9") ubyte test9_2;
__gshared extern pragma(mangle, "test\\\n9") ubyte test9_2_e;

//\a chars
__gshared pragma(mangle, "test\a9") ubyte test9_3;
__gshared extern pragma(mangle, "test\a9") ubyte test9_3_e;

//\x01 chars
__gshared pragma(mangle, "test\x019") ubyte test9_4;
__gshared extern pragma(mangle, "test\x019") ubyte test9_4_e;

//\xff chars
__gshared pragma(mangle, "test\xff9") ubyte test9_6;
__gshared extern pragma(mangle, "test\xff9") ubyte test9_6_e;

//unicode
__gshared pragma(mangle, "😀ÀÁÂÃÄÅßàáâãäaåbæçèéêcëìígîïð7ñ9__òóô4õöÆ3ÇÈÉÊËabcÌÍÎÏÐÑÒÓÔÕÖ😄😅🤣😂_ÿ")
ubyte test9_7;
__gshared extern pragma(mangle, "😀ÀÁÂÃÄÅßàáâãäaåbæçèéêcëìígîïð7ñ9__òóô4õöÆ3ÇÈÉÊËabcÌÍÎÏÐÑÒÓÔÕÖ😄😅🤣😂_ÿ")
ubyte test9_7_e;
