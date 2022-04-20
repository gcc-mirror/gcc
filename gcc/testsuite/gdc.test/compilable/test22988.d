// https://issues.dlang.org/show_bug.cgi?id=22988

enum a1 = 0;
enum b1 = a1 ? 1 << a1 - 1 : 0;

enum l = 0;
enum int[l] a2 = [];
enum b2 = l ? a2[l - 1] : 0;

enum a3 = 0 ? 1 << -1 : 0;

enum int[0] a4 = [];
enum b4 = 0 ? a4[0] : 0;

enum b5 = false ? (1 << -1) : 0;
