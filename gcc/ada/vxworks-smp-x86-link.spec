*lib:
+ %{mrtp:%{!shared: \
    %{vxsim: \
      -L%:getenv(WIND_BASE /target/lib_smp/usr/lib/simpentium/SIMPENTIUM/common) \
     } \
    %{!vxsim: \
      -L%:getenv(WIND_BASE /target/lib_smp/usr/lib/pentium/PENTIUM4/common) \
     } \
   }}
