/* Copyright (C) 2019-2020 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* The purpose of this file is to provide a compatiblity layer with
   the Linux kernel bpf_helpers.h header that is located in
   linux/tools/testing/selftests/bpf/bpf_helpers.h.  That file is
   currently llvm-specific.  */

#ifndef __BPF_HELPERS_H
#define __BPF_HELPERS_H

#define SEC(NAME) __attribute__((section(NAME), used))

/* Flags used in some kernel helpers.  */

#define BPF_ANY     0
#define BPF_NOEXIST 1
#define BPF_EXIST   2

#define BPF_F_LOCK 4
#define BPF_F_NO_COMMON_LRU (1U << 1)
#define BPF_F_NUMA_NODE (1U << 2)

/* Functions to call kernel helpers.  We provide the "standard" bpf_*
   names as synonyms of the corresponding GCC builtins.  In some
   cases, where non-void pointers are passed to the helper, inline
   functions are used to achieve proper type checking.  */

#ifndef KERNEL_VERSION
# define KERNEL_VERSION(a,b,c) (((a) << 16) + ((b) << 8) + (c))
#endif

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,0,0)

#define bpf_map_lookup_elem	__builtin_bpf_helper_map_lookup_elem
#define bpf_map_update_elem	__builtin_bpf_helper_map_update_elem
#define bpf_map_delete_elem	__builtin_bpf_helper_map_delete_elem

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,1,0)

#define bpf_probe_read		__builtin_bpf_helper_probe_read
#define bpf_ktime_get_ns	__builtin_bpf_helper_ktime_get_ns
#define bpf_trace_printk	__builtin_bpf_helper_trace_printk
#define bpf_get_prandom_u32	__builtin_bpf_helper_get_prandom_u32
#define bpf_get_smp_processor_id __builtin_bpf_helper_get_smp_processor_id
#define bpf_skb_store_bytes	__builtin_bpf_helper_skb_store_bytes
#define bpf_l3_csum_replace	__builtin_bpf_helper_l3_csum_replace
#define bpf_l4_csum_replace	__builtin_bpf_helper_l4_csum_replace

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,2,0)

#define bpf_tail_call		__builtin_bpf_helper_tail_call
#define bpf_clone_redirect	__builtin_bpf_helper_clone_redirect
#define bpf_get_current_pid_tgid __builtin_bpf_helper_get_current_pid_tgid
#define bpf_get_current_uid_gid  __builtin_bpf_helper_get_current_uid_gid
#define bpf_get_current_comm	__builtin_bpf_helper_get_current_comm

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,3,0)

#define bpf_get_cgroup_classid	__builtin_bpf_helper_get_cgroup_classid
#define bpf_skb_vlan_push	__builtin_bpf_helper_skb_vlan_push
#define bpf_skb_vlan_pop	__builtin_bpf_helper_skb_vlan_pop
#define bpf_skb_get_tunnel_key	__builtin_bpf_helper_skb_get_tunnel_key
#define bpf_skb_set_tunnel_key	__builtin_bpf_helper_skb_set_tunnel_key
#define bpf_perf_event_read	__builtin_bpf_helper_perf_event_read

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,4,0)

#define bpf_redirect		__builtin_bpf_helper_redirect
#define bpf_get_route_realm	__builtin_bpf_helper_get_route_realm
#define bpf_perf_event_output	__builtin_bpf_helper_perf_event_output

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,5,0)

#define bpf_skb_load_bytes	__builtin_bpf_helper_skb_load_bytes

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,6,0)

#define bpf_get_stackid		__builtin_bpf_helper_get_stackid
#define bpf_csum_diff		__builtin_bpf_helper_csum_diff
#define bpf_skb_get_tunnel_opt	__builtin_bpf_helper_skb_get_tunnel_opt
#define bpf_skb_set_tunnel_opt	__builtin_bpf_helper_skb_set_tunnel_opt

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,8,0)

#define bpf_skb_change_proto	__builtin_bpf_helper_skb_change_proto
#define bpf_skb_change_type	__builtin_bpf_helper_skb_change_type
#define bpf_skb_under_cgroup	__builtin_bpf_helper_skb_under_cgroup
#define bpf_get_hash_recalc	__builtin_bpf_helper_get_hash_recalc
#define bpf_get_current_task	__builtin_bpf_helper_get_current_task
#define bpf_probe_write_user	__builtin_bpf_helper_probe_write_user

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,9,0)

#define bpf_current_task_under_cgroup __builtin_bpf_helper_current_task_under_cgroup
#define bpf_skb_change_tail	__builtin_bpf_helper_skb_change_tail
#define bpf_skb_pull_data	__builtin_bpf_helper_skb_pull_data
#define bpf_csum_update		__builtin_bpf_helper_csum_update
#define bpf_set_hash_invalid	__builtin_bpf_helper_set_hash_invalid

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,10,0)

#define bpf_get_numa_node_id	__builtin_bpf_helper_get_numa_node_id
#define bpf_skb_change_head	__builtin_bpf_helper_skb_change_head
#define bpf_xdp_adjust_head	__builtin_bpf_helper_xdp_adjust_head

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,11,0)

#define bpf_probe_read_str	__builtin_bpf_helper_probe_read_str

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,12,0)

#define bpf_get_socket_cookie	__builtin_bpf_helper_get_socket_cookie
#define bpf_get_socket_uid	__builtin_bpf_helper_get_socket_uid

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,13,0)

#define bpf_set_hash		__builtin_bpf_helper_set_hash
#define bpf_setsockopt		__builtin_bpf_helper_setsockopt
#define bpf_skb_adjust_room	__builtin_bpf_helper_skb_adjust_room

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,14,0)

#define bpf_redirect_map	__builtin_bpf_helper_redirect_map
#define bpf_sk_redirect_map	__builtin_bpf_helper_sk_redirect_map
#define bpf_sock_map_update	__builtin_bpf_helper_sock_map_update

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,15,0)

#define bpf_perf_event_read_value __builtin_bpf_helper_perf_event_read_value
#define bpf_perf_prog_read_value  __builtin_bpf_helper_perf_prog_read_value
#define bpf_getsockopt		  __builtin_bpf_helper_getsockopt
#define bpf_xdp_adjust_meta	__builtin_bpf_helper_xdp_adjust_meta

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,16,0)

#define bpf_override_return	__builtin_bpf_helper_override_return
#define bpf_sock_ops_cb_flags_set __builtin_bpf_helper_sock_ops_cb_flags_set

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,17,0)

#define bpf_msg_redirect_map	__builtin_bpf_helper_msg_redirect_map
#define bpf_msg_apply_bytes	__builtin_bpf_helper_msg_apply_bytes
#define bpf_msg_cork_bytes	__builtin_bpf_helper_msg_cork_bytes
#define bpf_pull_data		__builtin_bpf_helper_pull_data
#define bpf_bind		__builtin_bpf_helper_bpf_bind

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,18,0)

#define bpf_xdp_adjust_tail	__builtin_bpf_helper_xdp_adjust_tail
#define bpf_skb_get_xfrm_state	__builtin_bpf_helper_skb_get_xfrm_state
#define bpf_get_stack		__builtin_bpf_helper_get_stack
#define bpf_skb_load_bytes_relative __builtin_bpf_helper_skb_load_bytes_relative
#define bpf_sock_hash_update	__builtin_bpf_helper_sock_hash_update
#define bpf_msg_redirect_hash	__builtin_bpf_helper_msg_redirect_hash
#define bpf_sk_redirect_hash	__builtin_bpf_helper_sk_redirect_hash
#define bpf_lwt_push_encap		__builtin_bpf_helper_lwt_push_encap
#define bpf_lwt_seg6_store_bytes	__builtin_bpf_helper_lwt_seg6_store_bytes
#define bpf_lwt_seg6_adjust_srh		__builtin_bpf_helper_lwt_seg6_adjust_srh
#define bpf_lwt_seg6_action		__builtin_bpf_helper_lwt_seg6_action
#define bpf_rc_repeat			__builtin_bpf_helper_rc_repeat
#define bpf_rc_keydown			__builtin_bpf_helper_rc_keydown
#define bpf_skb_cgroup_id		__builtin_bpf_helper_skb_cgroup_id
#define bpf_get_current_cgroup_id	__builtin_bpf_helper_get_current_cgroup_id

static inline int
bpf_fib_lookup (void *ctx, struct bpf_fib_lookup *param, int plen,
		unsigned int flags)
{
  return __builtin_bpf_helper_fib_lookup (ctx, (void *) param, plen, flags);
}


#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,19,0)

#define bpf_get_local_storage	__builtin_bpf_helper_get_local_storage
#define bpf_sk_select_reuseport	__builtin_bpf_helper_sk_select_reuseport
#define bpf_skb_ancestor_cgroup_id	__builtin_bpf_helper_skb_ancestor_cgroup_id

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (4,20,0)

#define bpf_sk_release		__builtin_bpf_helper_sk_release
#define bpf_map_push_elem	__builtin_bpf_helper_map_push_elem
#define bpf_map_pop_elem	__builtin_bpf_helper_map_pop_elem
#define bpf_map_peek_elem	__builtin_bpf_helper_map_peek_elem
#define bpf_msg_push_data	__builtin_bpf_helper_msg_push_data

static inline struct bpf_sock *
bpf_sk_lookup_tcp (void *ctx, struct bpf_sock_tuple *tuple,
		   int size, unsigned long long netns_id,
		   unsigned long long flags)
{
  return
    (struct bpf_sock *) __builtin_bpf_helper_sk_lookup_tcp (ctx,
							    (void *) tuple,
							    size,
							    netns_id, flags);
}

static inline struct bpf_sock *
bpf_sk_lookup_udp (void *ctx, struct bpf_sock_tuple *tuple,
		   int size, unsigned long long netns_id,
		   unsigned long long flags)
{
  return
    (struct bpf_sock *) __builtin_bpf_helper_sk_lookup_udp (ctx,
							    (void *) tuple,
							    size,
							    netns_id, flags);
}

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (5,0,0)

#define bpf_msg_pop_data	__builtin_bpf_helper_pop_data
#define bpf_rc_pointer_rel	__builtin_bpf_helper_rc_pointer_rel

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (5,1,0)

#define bpf_spin_lock		__builtin_bpf_helper_spin_lock
#define bpf_spin_unlock		__builtin_bpf_helper_spin_unlock
#define bpf_skb_ecn_set_ce	__builtin_bpf_helper_skb_ecn_set_ce

static inline struct bpf_sock *
bpf_sk_fullsock (struct bpf_sock *sk)
{
  return
    (struct bpf_sock *) __builtin_bpf_helper_sk_fullsock ((void *) sk);
}

static inline struct bpf_sock *
bpf_tcp_sock (struct bpf_sock *sk)
{
  return
    (struct bpf_sock *) __builtin_bpf_helper_tcp_sock ((void *) sk);
}

static inline struct bpf_sock *
bpf_get_listener_sock (struct bpf_sock *sk)
{
  return
    (struct bpf_sock *) __builtin_bpf_helper_get_listener_sock ((void *) sk);
}

#if __BPF_KERNEL_VERSION_CODE__ >= KERNEL_VERSION (5,2,0)


#endif /* 5.2 */
#endif /* 5.1 */
#endif /* 5.0 */
#endif /* 4.20 */
#endif /* 4.19 */
#endif /* 4.18 */
#endif /* 4.17 */
#endif /* 4.16 */
#endif /* 4.15 */
#endif /* 4.14 */
#endif /* 4.13 */
#endif /* 4.12 */
#endif /* 4.11 */
#endif /* 4.10 */
#endif /* 4.9 */
#endif /* 4.8 */
#endif /* 4.6 */
#endif /* 4.5 */
#endif /* 4.4 */
#endif /* 4.3 */
#endif /* 4.2 */
#endif /* 4.1 */
#endif /* 4.0 */

/* Functions to emit BPF_LD_ABS and BPF_LD_IND instructions.  We
   provide the "standard" names as synonyms of the corresponding GCC
   builtins.  Note how the SKB argument is ignored.  */

static inline long long
load_byte (void *skb __attribute__ ((unused)),
	   unsigned long long off)
{
  return __builtin_bpf_load_byte (off);
}

static inline long long
load_half (void *skb __attribute__ ((unused)),
	   unsigned long long off)
{
  return __builtin_bpf_load_half (off);
}

static inline long long
load_word (void *skb __attribute__ ((unused)),
	   unsigned long long off)
{
  return __builtin_bpf_load_word (off);
}

struct bpf_map_def
{
  unsigned int type;
  unsigned int key_size;
  unsigned int value_size;
  unsigned int max_entries;
  unsigned int map_flags;
  unsigned int inner_map_idx;
  unsigned int numa_node;
};

#endif /* ! __BPF_HELPERS_H */
