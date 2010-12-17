struct o_fsm_t;
struct o_fsm_event_t;

typedef void (*fn_t) (struct o_fsm_t *,
		      struct o_fsm_event_t const *);

struct o_fsm_state_t {
    fn_t dispatch;
};

struct o_fsm_t {
    fn_t dispatch;
};

extern struct o_fsm_state_t o_fsm_tran(struct o_fsm_t *fsm,
				       struct o_fsm_state_t next_state);
static void plist_parser_state_start(struct o_fsm_t *fsm,
				     struct o_fsm_event_t const *fsm_event);

struct o_fsm_state_t o_fsm_state(fn_t dispatch_fcn)
{
  return *(struct o_fsm_state_t *)&dispatch_fcn;
}

typedef struct _o_plist_parser_t {
    struct o_fsm_t fsm;
} o_plist_parser_t;

static void plist_parser_state_start(struct o_fsm_t *fsm,
				     struct o_fsm_event_t const *fsm_event)
{
}

void o_plist_deserialize_xml(int fin)
{
  o_plist_parser_t parser;
  o_fsm_tran(&parser.fsm, o_fsm_state(plist_parser_state_start));
}
