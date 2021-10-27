*self_spec:
+ %{!nostdlib:-nodefaultlibs -nostartfiles}

*link:
+ %{!nostdlib:%{mrtp:%{!shared: \
     -l:certRtp.o \
     -L%:getenv(VSB_DIR /usr/lib/common/objcert) \
     --defsym=__wrs_rtp_base=0x80000000 \
     -T%:getenv(VSB_DIR /usr/ldscripts/rtp.ld) \
   }}}
