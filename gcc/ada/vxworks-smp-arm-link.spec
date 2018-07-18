*lib:
+ %{mrtp:%{!shared: \
      -L%:getenv(WIND_BASE /target/lib_smp/usr/lib/arm/ARMARCH7/common) \
   }}
