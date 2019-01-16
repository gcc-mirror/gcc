/*
TEST_OUTPUT:
---
fail_compilation/fail2656.d(21): Error: octal literals `0123` are no longer supported, use `std.conv.octal!123` instead
fail_compilation/fail2656.d(22): Error: octal literals `01000000000000000000000` are no longer supported, use `std.conv.octal!1000000000000000000000` instead
fail_compilation/fail2656.d(23): Error: octal literals `0100000L` are no longer supported, use `std.conv.octal!100000L` instead
fail_compilation/fail2656.d(24): Error: octal literals `01777777777777777777777u` are no longer supported, use `std.conv.octal!1777777777777777777777u` instead
fail_compilation/fail2656.d(25): Error: octal literals `017777777777uL` are no longer supported, use `std.conv.octal!17777777777uL` instead
fail_compilation/fail2656.d(26): Error: octal literals `0177777` are no longer supported, use `std.conv.octal!177777` instead
fail_compilation/fail2656.d(27): Error: octal literals `020000000000L` are no longer supported, use `std.conv.octal!20000000000L` instead
fail_compilation/fail2656.d(28): Error: octal literals `0200000u` are no longer supported, use `std.conv.octal!200000u` instead
fail_compilation/fail2656.d(29): Error: octal literals `037777777777uL` are no longer supported, use `std.conv.octal!37777777777uL` instead
fail_compilation/fail2656.d(30): Error: octal literals `040000000000` are no longer supported, use `std.conv.octal!40000000000` instead
fail_compilation/fail2656.d(31): Error: octal literals `0777777777777777777777L` are no longer supported, use `std.conv.octal!777777777777777777777L` instead
fail_compilation/fail2656.d(32): Error: octal literals `077777u` are no longer supported, use `std.conv.octal!77777u` instead
fail_compilation/fail2656.d(33): Error: octal literals `077777uL` are no longer supported, use `std.conv.octal!77777uL` instead
fail_compilation/fail2656.d(34): Error: octal literals `077777uL` are no longer supported, use `std.conv.octal!77777uL` instead
---
*/

auto a = 0123;
auto b = 01000000000000000000000;
auto c = 0100000L;
auto d = 01777777777777777777777u;
auto e = 017777777777uL;
auto f = 0177777;
auto g = 020000000000L;
auto h = 0200000u;
auto i = 037777777777uL;
auto j = 040000000000;
auto k = 0777777777777777777777L;
auto l = 077777u;
auto m = 077777uL;
auto n = 0_7_7_7_7_7uL;
