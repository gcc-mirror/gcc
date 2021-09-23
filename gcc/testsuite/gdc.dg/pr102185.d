// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=102185
// { dg-do compile }

static assert(__traits(getTargetInfo, "floatAbi").length == 0 ||
              __traits(getTargetInfo, "floatAbi") == "hard" ||
              __traits(getTargetInfo, "floatAbi") == "soft" ||
              __traits(getTargetInfo, "floatAbi") == "softfp");
