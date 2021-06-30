#!/bin/bash

inViolation="false"
sed 's/^/:/' /dev/stdin | while read -r line; do
  line="$(echo "$line" | sed 's/^://')"

  if [[ "${inViolation}" == "false" ]]; then
    echo "$line"
    if [[ -n "$(echo "$line" | grep 'violation occurs here:')" ]]; then
      inViolation="true"
    fi
    continue
  fi

  if [[ -n "$(echo "$line" | grep 'end of violation')" ]]; then
    inViolation="false"
    continue
  fi

  addr="$(echo "$line" | sed -r 's/.*\[0x([a-f0-9]+)\]$/\1/')"
  bin="$(echo "$line" | sed -r 's/^([^([]*).*/\1/')"
  [[ -n "${bin}" ]] || continue
  t="$(addr2line -e "$bin" "$addr" -f)"
  file="$(echo "$t" | tail -1 | tr ':' '\n' | head -1)"
  file="$(echo "$file" | sed -r "s:^$(pwd)/?::")"
  line="$(echo "$t" | tail -1 | tr ':' '\n' | tail -1 | cut -d' ' -f1)"
  func="$(echo "$t" | head -1 | c++filt)"
  [[ $file != "??" ]] && echo "    $file:$func:$line"
done

