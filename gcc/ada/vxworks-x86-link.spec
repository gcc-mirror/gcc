*lib:
+ %{mrtp:%{!shared: \
    %{vxsim: \
      -L%:getenv(WIND_BASE /target/usr/lib/simpentium/SIMPENTIUM/common) \
      -L%:getenv(WIND_BASE /target/lib/usr/lib/simpentium/SIMPENTIUM/common) \
     } \
    %{!vxsim: \
      -L%:getenv(WIND_BASE /target/usr/lib/pentium/PENTIUM/common) \
      -L%:getenv(WIND_BASE /target/lib/usr/lib/pentium/PENTIUM/common) \
     } \
   }}
