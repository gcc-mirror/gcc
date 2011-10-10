set height 0
break simulate_thread_main
disp/i $pc
run

set $ret = 0
while (simulate_thread_fini != 1) && (! $ret)
  call simulate_thread_wrapper_other_threads()
  stepi
  set $ret |= simulate_thread_step_verify()
end

if (! $ret)
  set $ret |= simulate_thread_wrapper_final_verify()
end
continue
quit $ret
